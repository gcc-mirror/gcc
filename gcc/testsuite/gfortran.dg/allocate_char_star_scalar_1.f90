! { dg-do compile }
! Tests the patch for PR26038 that used to ICE in gfc_trans_allocate
! for the want of a string_length to pass to the library.
! Contributed by hjl@lucon.org && Erik Edelmann  <eedelmanncc.gnu.org>
module moo

contains

    subroutine foo(self)
        character(*) :: self
        pointer :: self

        nullify(self)
        allocate(self)          ! Used to ICE here
        print *, len(self)
    end subroutine

end module moo


program hum

    use moo

    character(5), pointer :: p
    character(10), pointer :: q

    call foo(p)
    call foo(q)

end program hum

! { dg-final { cleanup-modules "moo" } }
