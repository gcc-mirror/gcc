! { dg-do compile }
! PR fortran/71565 - INTENT(IN) polymorphic argument with pointer components
!
! Contributed by Marco Restelli.

module m
  implicit none

  type, abstract :: t_a
  end type t_a

  type, extends(t_a), abstract :: t_b
     integer, pointer :: i => null()
  end type t_b

contains

  subroutine s1(var)
    class(t_a), intent(in) :: var
    select type(var)
    class is(t_b)
       var%i = 3
       var%i => NULL()      ! { dg-error "pointer association context" }
    end select
  end subroutine s1

  subroutine s1a(var)
    class(t_a), intent(in) :: var
    select type(tmp => var) ! { dg-error "variable definition context" }
    class is(t_b)
       tmp%i = 3
       tmp%i => NULL()      ! { dg-error "variable definition context" }
    end select
  end subroutine s1a

  subroutine s2(var)
    class(t_b), intent(in) :: var
    var%i = 3
    var%i => NULL()        ! { dg-error "pointer association context" }
  end subroutine s2

  subroutine s2a(var)
    class(t_b), intent(in) :: var
    associate (tmp => var) ! { dg-error "variable definition context" }
      print *, associated (tmp%i)
      tmp%i = 3
      tmp%i => NULL()      ! { dg-error "variable definition context" }
    end associate
  end subroutine s2a

  subroutine s2b(var)
    class(t_b), intent(in) :: var
    associate (tmp => var%i)
      tmp = 3
    end associate
  end subroutine s2b

  subroutine s3(var)
    class(t_a), intent(in) :: var
    integer, pointer :: tmp
    select type(var); class is(t_b)
       tmp => var%i
       tmp =  3
    end select
  end subroutine s3

end module m
