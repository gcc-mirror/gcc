! { dg-do run }
! This checks the fix for PR32103 in which not using one member
! of an equivalence group would cause all memory of the equivalence
! to be lost and subsequent incorrect referencing of the remaining
! members. 
!
! Contributed by Toon Moene <toon@moene.indiv.nluug.nl> 
!
module aap
   real :: a(5) = (/1.0,2.0,3.0,4.0,5.0/) 
   real :: b(3)
   real :: d(5) = (/1.0,2.0,3.0,4.0,5.0/) 
   equivalence (a(3),b(1))
end module aap

  use aap, only : b
  call foo
  call bar
!  call foobar
contains
  subroutine foo
    use aap, only : c=>b
    if (any(c .ne. b)) call abort ()
  end subroutine
  subroutine bar
    use aap, only : a
    if (any(a(3:5) .ne. b)) call abort ()
  end subroutine

! Make sure that bad things do not happen if we do not USE a or b.

  subroutine foobar
    use aap, only : d
    if (any(d(3:5) .ne. b)) call abort ()
  end subroutine
end

! { dg-final { cleanup-modules "aap" } }
