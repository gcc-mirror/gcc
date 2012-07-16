! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR 53824 - this used to ICE.
! Original test case by Vladimír Fuka
program Jac
 implicit none

 integer,parameter:: KND=KIND(1.0)

 type Domain
  real(KND),dimension(:,:,:),allocatable:: A,B
  integer :: n=64,niter=20000,blockit=1000
  integer :: starti,endi
  integer :: startj,endj
  integer :: startk,endk
  integer,dimension(:),allocatable :: startsi,startsj,startsk
  integer,dimension(:),allocatable :: endsi,endsj,endsk
 end type

 type(Domain),allocatable :: D[:,:,:]
! real(KND),codimension[*] :: sumA,sumB,diffAB
 integer i,j,k,ncom
 integer nims,nxims,nyims,nzims
 integer im,iim,jim,kim
 character(20):: ch

 nims = num_images()
 nxims = nint(nims**(1./3.))
 nyims = nint(nims**(1./3.))
 nzims = nims / (nxims*nyims)

 im = this_image()
 if (im==1) write(*,*) "n: [",nxims,nyims,nzims,"]"

 kim = (im-1) / (nxims*nyims) + 1
 jim = ((im-1) - (kim-1)*(nxims*nyims)) / nxims + 1
 iim = (im-1) - (kim-1)*(nxims*nyims) - (jim-1)*(nxims) + 1

 write (*,*) im,"[",iim,jim,kim,"]"

 allocate(D[nxims,nyims,*])

 ncom=command_argument_count()
 if (command_argument_count() >=2) then
  call get_command_argument(1,value=ch)
  read (ch,*) D%n
  call get_command_argument(2,value=ch)
  read (ch,*) D%niter
  call get_command_argument(3,value=ch)
  read (ch,*) D%blockit
 end if

 allocate(D%startsi(nxims))
 allocate(D%startsj(nyims))
 allocate(D%startsk(nzims))
 allocate(D%endsi(nxims))
 allocate(D%endsj(nyims))
 allocate(D%endsk(nzims))

 D%startsi(1) = 1
 do i=2,nxims
   D%startsi(i) = D%startsi(i-1) + D%n/nxims
 end do
 D%endsi(nxims) = D%n
 D%endsi(1:nxims-1) = D%startsi(2:nxims) - 1

 D%startsj(1) = 1
 do j=2,nyims
   D%startsj(j) = D%startsj(j-1) + D%n/nyims
 end do
 D%endsj(nyims) = D%n
 D%endsj(1:nyims-1) = D%startsj(2:nyims) - 1

 D%startsk(1) = 1
 do k=2,nzims
   D%startsk(k) = D%startsk(k-1) + D%n/nzims
 end do
 D%endsk(nzims) = D%n
 D%endsk(1:nzims-1) = D%startsk(2:nzims) - 1

 D%starti = D%startsi(iim)
 D%endi = D%endsi(iim)
 D%startj = D%startsj(jim)
 D%endj = D%endsj(jim)
 D%startk = D%startsk(kim)
 D%endk = D%endsk(kim)

 write(*,*) D%startsi,D%endsi
 write(*,*) D%startsj,D%endsj
 write(*,*) D%startsk,D%endsk

 !$hmpp JacKernel allocate, args[A,B].size={0:D%n+1,0:D%n+1,0:D%n+1}
 allocate(D%A(D%starti-1:D%endi+1,D%startj-1:D%endj+1,D%startk-1:D%endk+1),&
  D%B(D%starti-1:D%endi+1,D%startj-1:D%endj+1,D%startk-1:D%endk+1))
end program Jac
