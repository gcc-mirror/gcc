! { dg-do run }
!
! PR 45420: [OOP] polymorphic TBP call in a CLASS DEFAULT clause
!
! Contributed by Salvatore Filippone <salvatore.filippone@uniroma2.it>


module base_mat_mod

 type  :: base_sparse_mat
 contains
   procedure, pass(a) :: get_fmt => base_get_fmt
 end type base_sparse_mat

contains

 function base_get_fmt(a) result(res)
   implicit none
   class(base_sparse_mat), intent(in) :: a
   character(len=5) :: res
   res = 'NULL'
 end function base_get_fmt

end module base_mat_mod


module d_base_mat_mod

 use base_mat_mod

 type, extends(base_sparse_mat) :: d_base_sparse_mat
 contains
   procedure, pass(a) :: get_fmt => d_base_get_fmt
 end type d_base_sparse_mat

 type, extends(d_base_sparse_mat) :: x_base_sparse_mat
 contains
   procedure, pass(a) :: get_fmt => x_base_get_fmt
 end type x_base_sparse_mat

contains

 function d_base_get_fmt(a) result(res)
   implicit none
   class(d_base_sparse_mat), intent(in) :: a
   character(len=5) :: res
   res = 'DBASE'
 end function d_base_get_fmt

 function x_base_get_fmt(a) result(res)
   implicit none
   class(x_base_sparse_mat), intent(in) :: a
   character(len=5) :: res
   res = 'XBASE'
 end function x_base_get_fmt

end module d_base_mat_mod


program bug20
  use d_base_mat_mod
  class(d_base_sparse_mat), allocatable  :: a

  allocate(x_base_sparse_mat :: a)
  if (a%get_fmt()/="XBASE") call abort()

  select type(a)
  type is (d_base_sparse_mat)
    call abort()
  class default
    if (a%get_fmt()/="XBASE") call abort()
  end select

end program bug20
