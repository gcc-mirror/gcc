! { dg-do compile }
! Tests the fix for PR30531 in which the interface derived types
! was not being associated.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module foo_type_mod
  type foo_type
     integer, allocatable :: md(:)
  end type foo_type
end module foo_type_mod

module foo_mod

  interface
    subroutine foo_initvg(foo_a)
      use foo_type_mod
      Type(foo_type), intent(out) :: foo_a
    end subroutine foo_initvg
  end interface

contains

  subroutine foo_ext(foo_a)
    use foo_type_mod
    Type(foo_type) :: foo_a

    call foo_initvg(foo_a)
  end subroutine foo_ext

end module foo_mod
! { dg-final { cleanup-modules "foo_type_mod foo_mod" } }
