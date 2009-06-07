! { dg-do run }
! Transformational functions for zero-sized array and array sections
! Contributed by Francois-Xavier Coudert  <coudert@clipper.ens.fr>

subroutine test_cshift
  real :: tempn(1), tempm(1,2)
  real,allocatable :: foo(:),bar(:,:),gee(:,:)
  tempn = 2.0
  tempm = 1.0
  allocate(foo(0),bar(2,0),gee(0,7))
  if (any(cshift(foo,dim=1,shift=1)/= 0)) call abort
  if (any(cshift(tempn(2:1),dim=1,shift=1)/= 0)) call abort
  if (any(cshift(bar,shift=(/1,-1/),dim=1)/= 0)) call abort
  if (any(cshift(bar,shift=(/1,-1/),dim=2)/= 0)) call abort
  if (any(cshift(gee,shift=(/1,-1/),dim=1)/= 0)) call abort
  if (any(cshift(gee,shift=(/1,-1/),dim=2)/= 0)) call abort
  if (any(cshift(tempm(5:4,:),shift=(/1,-1/),dim=1)/= 0)) call abort
  deallocate(foo,bar,gee)
end

subroutine test_eoshift
  real :: tempn(1), tempm(1,2)
  real,allocatable :: foo(:),bar(:,:),gee(:,:)
  tempn = 2.0
  tempm = 1.0
  allocate(foo(0),bar(2,0),gee(0,7))
  if (any(eoshift(foo,dim=1,shift=1)/= 0)) call abort
  if (any(eoshift(tempn(2:1),dim=1,shift=1)/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=1)/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=2)/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=1)/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=2)/= 0)) call abort
  if (any(eoshift(tempm(5:4,:),shift=(/1,-1/),dim=1)/= 0)) call abort

  if (any(eoshift(foo,dim=1,shift=1,boundary=42.0)/= 0)) call abort
  if (any(eoshift(tempn(2:1),dim=1,shift=1,boundary=42.0)/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=1,boundary=42.0)/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=2,boundary=42.0)/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=1,boundary=42.0)/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=2,boundary=42.0)/= 0)) call abort
  if (any(eoshift(tempm(5:4,:),shift=(/1,-1/),dim=1,boundary=42.0)/= 0)) call abort

  if (any(eoshift(foo,dim=1,shift=1,boundary=42.0)/= 0)) call abort
  if (any(eoshift(tempn(2:1),dim=1,shift=1,boundary=-7.0)/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=1,boundary=(/42.0,-7.0/))/= 0)) call abort
  if (any(eoshift(bar,shift=(/1,-1/),dim=2,boundary=(/42.0,-7.0/))/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=1,boundary=(/42.0,-7.0/))/= 0)) call abort
  if (any(eoshift(gee,shift=(/1,-1/),dim=2,boundary=(/42.0,-7.0/))/= 0)) call abort
  if (any(eoshift(tempm(5:4,:),shift=(/1,-1/),dim=1,boundary=(/42.0,-7.0/))/= 0)) call abort
  deallocate(foo,bar,gee)
end

subroutine test_transpose
  character(len=1) :: tempn(1,2)
  character(len=1),allocatable :: foo(:,:), bar(:,:)
  integer :: tempm(1,2)
  integer,allocatable :: x(:,:), y(:,:)
  tempn = 'a'
  allocate(foo(3,0),bar(-2:-4,7:9))
  tempm = -42
  allocate(x(3,0),y(-2:-4,7:9))
  if (any(transpose(tempn(-7:-8,:)) /= 'b')) call abort
  if (any(transpose(tempn(:,9:8)) /= 'b')) call abort
  if (any(transpose(foo) /= 'b')) call abort
  if (any(transpose(bar) /= 'b')) call abort
  if (any(transpose(tempm(-7:-8,:)) /= 0)) call abort
  if (any(transpose(tempm(:,9:8)) /= 0)) call abort
  if (any(transpose(x) /= 0)) call abort
  if (any(transpose(y) /= 0)) call abort
  deallocate(foo,bar,x,y)
end

subroutine test_reshape
  character(len=1) :: tempn(1,2)
  character(len=1),allocatable :: foo(:,:), bar(:,:)
  integer :: tempm(1,2)
  integer,allocatable :: x(:,:), y(:,:)
  tempn = 'b'
  tempm = -42
  allocate(foo(3,0),bar(-2:-4,7:9),x(3,0),y(-2:-4,7:9))
  
  if (size(reshape(tempn(-7:-8,:),(/3,3/),pad=(/'a'/))) /= 9 .or. &
      any(reshape(tempn(-7:-8,:),(/3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(tempn(-7:-8,:),(/3,3,3/),pad=(/'a'/))) /= 27 .or. &
      any(reshape(tempn(-7:-8,:),(/3,3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(tempn(-7:-8,:),(/3,3,3,3,3,3,3/),pad=(/'a'/))) /= 2187 .or. &
      any(reshape(tempn(-7:-8,:),(/3,3,3,3,3,3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(foo,(/3,3/),pad=(/'a'/))) /= 9 .or. &
      any(reshape(foo,(/3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(foo,(/3,3,3/),pad=(/'a'/))) /= 27 .or. &
      any(reshape(foo,(/3,3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(foo,(/3,3,3,3,3,3,3/),pad=(/'a'/))) /= 2187 .or. &
      any(reshape(foo,(/3,3,3,3,3,3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(bar,(/3,3/),pad=(/'a'/))) /= 9 .or. &
      any(reshape(bar,(/3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(bar,(/3,3,3/),pad=(/'a'/))) /= 27 .or. &
      any(reshape(bar,(/3,3,3/),pad=(/'a'/)) /= 'a')) call abort
  if (size(reshape(bar,(/3,3,3,3,3,3,3/),pad=(/'a'/))) /= 2187 .or. &
      any(reshape(bar,(/3,3,3,3,3,3,3/),pad=(/'a'/)) /= 'a')) call abort

  if (size(reshape(tempm(-7:-8,:),(/3,3/),pad=(/7/))) /= 9 .or. &
      any(reshape(tempm(-7:-8,:),(/3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(tempm(-7:-8,:),(/3,3,3/),pad=(/7/))) /= 27 .or. &
      any(reshape(tempm(-7:-8,:),(/3,3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(tempm(-7:-8,:),(/3,3,3,3,3,3,3/),pad=(/7/))) /= 2187 .or. &
      any(reshape(tempm(-7:-8,:),(/3,3,3,3,3,3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(x,(/3,3/),pad=(/7/))) /= 9 .or. &
      any(reshape(x,(/3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(x,(/3,3,3/),pad=(/7/))) /= 27 .or. &
      any(reshape(x,(/3,3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(x,(/3,3,3,3,3,3,3/),pad=(/7/))) /= 2187 .or. &
      any(reshape(x,(/3,3,3,3,3,3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(y,(/3,3/),pad=(/7/))) /= 9 .or. &
      any(reshape(y,(/3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(y,(/3,3,3/),pad=(/7/))) /= 27 .or. &
      any(reshape(y,(/3,3,3/),pad=(/7/)) /= 7)) call abort
  if (size(reshape(y,(/3,3,3,3,3,3,3/),pad=(/7/))) /= 2187 .or. &
      any(reshape(y,(/3,3,3,3,3,3,3/),pad=(/7/)) /= 7)) call abort

  deallocate(foo,bar,x,y)
end

subroutine test_pack
  integer :: tempn(1,5)
  integer,allocatable :: foo(:,:)
  tempn = 2 
  allocate(foo(0,1:7))
  if (size(pack(foo,foo/=0)) /= 0 .or. any(pack(foo,foo/=0) /= -42)) call abort
  if (size(pack(foo,foo/=0,(/1,3,4,5,1,0,7,9/))) /= 8 .or. &
      sum(pack(foo,foo/=0,(/1,3,4,5,1,0,7,9/))) /= 30) call abort
  if (size(pack(tempn(:,-4:-5),tempn(:,-4:-5)/=0)) /= 0 .or. &
      any(pack(tempn(:,-4:-5),tempn(:,-4:-5)/=0) /= -42)) call abort
  if (size(pack(tempn(:,-4:-5),tempn(:,-4:-5)/=0,(/1,3,4,5,1,0,7,9/))) /= 8 .or. &
      sum(pack(tempn(:,-4:-5),tempn(:,-4:-5)/=0,(/1,3,4,5,1,0,7,9/))) /= 30) &
    call abort
  if (size(pack(foo,.true.)) /= 0 .or. any(pack(foo,.true.) /= -42)) &
    call abort
  if (size(pack(foo,.true.,(/1,3,4,5,1,0,7,9/))) /= 8 .or. &
      sum(pack(foo,.true.,(/1,3,4,5,1,0,7,9/))) /= 30) call abort
  if (size(pack(tempn(:,-4:-5),.true.)) /= 0 .or. &
      any(pack(foo,.true.) /= -42)) call abort
  if (size(pack(tempn(:,-4:-5),.true.,(/1,3,4,5,1,0,7,9/))) /= 8 .or. &
      sum(pack(tempn(:,-4:-5),.true.,(/1,3,4,5,1,0,7,9/))) /= 30) call abort
  deallocate(foo)
end

subroutine test_unpack
  integer :: tempn(1,5), tempv(5)
  integer,allocatable :: foo(:,:), bar(:)
  integer :: zero
  tempn = 2 
  tempv = 5
  zero = 0
  allocate(foo(0,1:7),bar(0:-1))
  if (any(unpack(tempv,tempv/=0,tempv) /= 5) .or. &
      size(unpack(tempv,tempv/=0,tempv)) /= 5) call abort
  if (any(unpack(tempv(1:0),tempv/=0,tempv) /= 5) .or. &
      size(unpack(tempv(1:0),tempv/=0,tempv)) /= 5) call abort
  if (any(unpack(tempv,tempv(1:zero)/=0,tempv) /= -47)) call abort
  if (any(unpack(tempv(5:4),tempv(1:zero)/=0,tempv) /= -47)) call abort
  if (any(unpack(bar,foo==foo,foo) /= -47)) call abort
  deallocate(foo,bar)
end

subroutine test_spread
  real :: tempn(1)
  real,allocatable :: foo(:)
  tempn = 2.0 
  allocate(foo(0))
  if (any(spread(1,dim=1,ncopies=0) /= -17.0) .or. &
      size(spread(1,dim=1,ncopies=0)) /= 0) call abort
  if (any(spread(foo,dim=1,ncopies=1) /= -17.0) .or. &
      size(spread(foo,dim=1,ncopies=1)) /= 0) call abort
  if (any(spread(tempn(2:1),dim=1,ncopies=1) /= -17.0) .or. &
      size(spread(tempn(2:1),dim=1,ncopies=1)) /= 0) call abort
  deallocate(foo)
end

program test
  call test_cshift
  call test_eoshift
  call test_transpose
  call test_unpack
  call test_spread
  call test_pack
  call test_reshape
end
