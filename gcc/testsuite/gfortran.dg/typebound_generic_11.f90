! { dg-do compile }
!
! PR fortran/52024
!
! Contributed by Fran Martinez Fadrique
!
module m_test
  type t_test
    integer :: i = 0
  contains
    generic :: operator(==) => t_equal_i, i_equal_t ! OK
    procedure, private          :: t_equal_i
    procedure, private, pass(t) :: i_equal_t
  end type t_test
contains
  function t_equal_i (t, i) result(res)
    class(t_test), intent(in) :: t
    integer,       intent(in) :: i
    logical :: res

    print *, 't_equal_i', t%i, i  
    res = ( t%i == i )
  end function t_equal_i

  function i_equal_t (i, t) result(res)
    integer,       intent(in) :: i
    class(t_test), intent(in) :: t
    logical :: res
  
    print *, 'i_equal_t', i, t%i
    res = ( t%i == i )
  end function i_equal_t
end module m_test

module m_test2
  type t2_test
    integer :: i = 0
  contains
    generic :: gen => t2_equal_i, i_equal_t2 ! { dg-error "'t2_equal_i' and 'i_equal_t2' for GENERIC 'gen' at .1. are ambiguous" }
    procedure, private          :: t2_equal_i
    procedure, private, pass(t) :: i_equal_t2
  end type t2_test
contains
  function t2_equal_i (t, i) result(res)
    class(t2_test), intent(in) :: t
    integer,        intent(in) :: i
    logical :: res

    print *, 't2_equal_i', t%i, i  
    res = ( t%i == i )
  end function t2_equal_i

  function i_equal_t2 (i, t) result(res)
    integer,        intent(in) :: i
    class(t2_test), intent(in) :: t
    logical :: res
  
    print *, 'i_equal_t2', i, t%i
    res = ( t%i == i )
  end function i_equal_t2
end module m_test2

! { dg-final { cleanup-modules "m_test m_test2" } }
