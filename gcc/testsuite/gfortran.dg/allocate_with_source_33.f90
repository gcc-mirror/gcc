! { dg-do compile }
! { dg-options "-O0" }
!
! PR fortran/114019 - allocation with source of deferred character length

subroutine s
  implicit none
  character(1)              :: w   = "4"
  character(*), parameter   :: str = "123"
  character(5), pointer     :: chr_pointer1
  character(:), pointer     :: chr_pointer2
  character(:), pointer     :: chr_ptr_arr(:)
  character(5), allocatable :: chr_alloc1
  character(:), allocatable :: chr_alloc2
  character(:), allocatable :: chr_all_arr(:)
  allocate (chr_pointer1, source=w// str//w)
  allocate (chr_pointer2, source=w// str//w)
  allocate (chr_ptr_arr,  source=w//[str//w])
  allocate (chr_alloc1,   source=w// str//w)
  allocate (chr_alloc2,   source=w// str//w)
  allocate (chr_all_arr,  source=w//[str//w])
  allocate (chr_pointer2, source=str)
  allocate (chr_pointer2, source=w)
  allocate (chr_alloc2,   source=str)
  allocate (chr_alloc2,   source=w)
  allocate (chr_pointer1, mold  =w// str//w)
  allocate (chr_pointer2, mold  =w// str//w)
  allocate (chr_ptr_arr,  mold  =w//[str//w])
  allocate (chr_alloc1,   mold  =w// str//w)
  allocate (chr_alloc2,   mold  =w// str//w)
  allocate (chr_all_arr,  mold  =w//[str//w])
  allocate (chr_pointer2, mold  =str)
  allocate (chr_pointer2, mold  =w)
  allocate (chr_alloc2,   mold  =str)
  allocate (chr_alloc2,   mold  =w)
end

subroutine s2
  implicit none
  integer, parameter :: ck=4
  character(kind=ck,len=1)              :: w   = ck_"4"
  character(kind=ck,len=*), parameter   :: str = ck_"123"
  character(kind=ck,len=5), pointer     :: chr_pointer1
  character(kind=ck,len=:), pointer     :: chr_pointer2
  character(kind=ck,len=:), pointer     :: chr_ptr_arr(:)
  character(kind=ck,len=5), allocatable :: chr_alloc1
  character(kind=ck,len=:), allocatable :: chr_alloc2
  character(kind=ck,len=:), allocatable :: chr_all_arr(:)
  allocate (chr_pointer1, source=w// str//w)
  allocate (chr_pointer2, source=w// str//w)
  allocate (chr_ptr_arr,  source=w//[str//w])
  allocate (chr_alloc1,   source=w// str//w)
  allocate (chr_alloc2,   source=w// str//w)
  allocate (chr_all_arr,  source=w//[str//w])
  allocate (chr_pointer2, source=str)
  allocate (chr_pointer2, source=w)
  allocate (chr_alloc2,   source=str)
  allocate (chr_alloc2,   source=w)
  allocate (chr_pointer1, mold  =w// str//w)
  allocate (chr_pointer2, mold  =w// str//w)
  allocate (chr_ptr_arr,  mold  =w//[str//w])
  allocate (chr_alloc1,   mold  =w// str//w)
  allocate (chr_alloc2,   mold  =w// str//w)
  allocate (chr_all_arr,  mold  =w//[str//w])
  allocate (chr_pointer2, mold  =str)
  allocate (chr_pointer2, mold  =w)
  allocate (chr_alloc2,   mold  =str)
  allocate (chr_alloc2,   mold  =w)
end
