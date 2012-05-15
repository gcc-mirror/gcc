! { dg-do run }
!
! PR 44065: [OOP] Undefined reference to vtab$...
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module s_mat_mod
  implicit none 
  type :: s_sparse_mat
  end type
contains
  subroutine s_set_triangle(a)
    class(s_sparse_mat), intent(inout) :: a
  end subroutine
end module

module s_tester
implicit none
contains
  subroutine s_ussv_2
    use s_mat_mod
    type(s_sparse_mat) :: a
    call s_set_triangle(a)
  end subroutine
end module

end
 
