! { dg-do run }
! Tests the fix for pr31609, where module procedure entries found
! themselves in the wrong namespace.  This test checks that all
! combinations of generic and specific calls work correctly.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org> as comment #8 to the pr.
!
MODULE ksbin1_aux_mod
  interface foo
    module procedure j
  end interface
  interface bar
    module procedure k
  end interface
  interface foobar
    module procedure j, k
  end interface
  CONTAINS
    FUNCTION j () 
    j = 1
    return
    ENTRY k (i) 
    k = 2
    END FUNCTION j
END MODULE ksbin1_aux_mod

    use ksbin1_aux_mod
    if (any ((/foo (), bar (99), foobar (), foobar (99), j (), k (99)/) .ne. &
             (/1, 2, 1, 2, 1, 2/))) Call abort ()
end
