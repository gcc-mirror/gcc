! { dg-do run }
! Tests the fix for PR24558, which reported that module
! alternate function entries did not work.
!
! Contributed by Erik Edelmann  <eedelman@gcc.gnu.org>
!
module foo
contains
    function n1 (a)
        integer :: n1, n2, a, b
        integer, save :: c
        c = a
        n1 = c**3
        return
    entry n2 (b)
        n2 = c * b
        n2 = n2**2
        return
    end function n1
    function z1 (u)
        complex :: z1, z2, u, v
        z1 = (1.0, 2.0) * u
        return
    entry z2 (v)
        z2 = (3, 4) * v
        return
    end function z1
    function n3 (d)
        integer :: n3, d
        n3 = n2(d) * n1(d) ! Check sibling references.
        return
    end function n3
    function c1 (a)
        character(4) :: c1, c2, a, b
        c1 = a
        if (a .eq. "abcd") c1 = "ABCD"
        return
    entry c2 (b)
        c2 = b
        if (b .eq. "wxyz") c2 = "WXYZ"
        return
    end function c1
end module foo
    use foo
    if (n1(9) .ne. 729) call abort ()
    if (n2(2) .ne. 324) call abort ()
    if (n3(19) .ne. 200564019) call abort ()
    if (c1("lmno") .ne. "lmno") call abort ()
    if (c1("abcd") .ne. "ABCD") call abort ()
    if (c2("lmno") .ne. "lmno") call abort ()
    if (c2("wxyz") .ne. "WXYZ") call abort ()
    if (z1((3,4)) .ne. (-5, 10)) call abort ()
    if (z2((5,6)) .ne. (-9, 38)) call abort ()
 end

! { dg-final { cleanup-modules "foo" } }
