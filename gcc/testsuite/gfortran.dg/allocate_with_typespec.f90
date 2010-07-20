!
! { dg-do compile }
!
! PR fortran/44929
!
! The module is contributed by Satish.BD <bdsatish@gmail.com>.
! The subroutines are from Tobias Burnus and Steve Kargl.
!
module temp

   type, abstract :: abst
      !! empty
   end type abst

   type, extends(abst) :: real_type
      !! empty
   end type real_type

   contains

   function create(name)  result(obj)
      character(len=*), intent(in) :: name
      class(abst), pointer :: obj
      allocate(real_type :: obj)
   end function create
end module temp

subroutine z
   real(8), allocatable :: r8
   allocate(real(kind=8) :: r8)
end subroutine z

subroutine y
   real(8), allocatable :: r8
   allocate(real(8) :: r8)
end subroutine y
! { dg-final { cleanup-modules "temp" } }

