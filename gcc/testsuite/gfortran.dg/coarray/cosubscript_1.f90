! { dg-do run }
!
! From the HPCTools Group of University of Houston
!
! For a coindexed object, its cosubscript list determines the image
! index in the same way that a subscript list determines the subscript
! order value for an array element

! Run at least with 3 images for the normal checking code
! Modified to also accept a single or two images
program cosubscript_test
  implicit none
  
  integer, parameter :: X = 3, Y = 2
  integer, parameter :: P = 1, Q = -1
  integer :: me
  integer :: i,j,k
  
  integer :: scalar[0:P, -1:Q, *]
  
  integer :: dim3_max, counter
  logical :: is_err
  
  is_err = .false.
  me = this_image()
  scalar   = me
  dim3_max = num_images() / ( (P+1)*(Q+2) )
  
  sync all

  if (num_images() == 1) then
    k = 1
    j = -1
    i = 0
    if (scalar[i,j,k] /= this_image()) STOP 1
    stop "OK"
  else if (num_images() == 2) then
    k = 1
    j = -1
    counter = 0
    do i = 0,P
      counter = counter+1
      if (counter /= scalar[i,j,k]) STOP 1
    end do
    stop "OK"
  end if

  ! ******* SCALAR ***********
  counter = 0
  do k = 1, dim3_max
     do j = -1,Q
        do i = 0,P
           counter = counter+1
           if (counter /= scalar[i,j,k]) then
              print * , "Error in cosubscript translation scalar"
              print * , "[", i,",",j,",",k,"] = ",scalar[i,j,k],"/=",counter
              is_err = .true.
           end if
        end do
     end do
  end do
  
  if (is_err) then
    STOP 2
  end if
end program cosubscript_test
