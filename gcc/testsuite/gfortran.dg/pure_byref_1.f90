! { dg-do run }
! PR 22607: PURE/ELEMENTAL return-by-reference functions
program main
  implicit none
  character(2), dimension(2) :: a, b
  a = 'ok'
  b = fun(a)
  if (.not.all(b == 'ok')) call abort()
contains
  elemental function fun(a) 
    character(*), intent(in) :: a
    character(len(a)) :: fun
    fun = a
  end function fun
end program main
