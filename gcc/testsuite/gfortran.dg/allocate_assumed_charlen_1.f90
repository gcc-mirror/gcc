! { dg-do run }
!
! PR82934: Segfault on compilation in trans-stmt.c:5919(8.0.0).
! The original report only had one item in the allocate list. This
! has been doubled up to verify that the correct string length is
! is used in the allocation.
!
! Contributed by FortranFan on clf.
!
   character(len=42), allocatable :: foo
   character(len=22), allocatable :: foofoo

   call alloc( foo , foofoo)

   if (len(foo) .ne. 42) STOP 1
   if (len(foofoo) .ne. 22) STOP 2

contains

   subroutine alloc( bar, barbar )

      character(len=*), allocatable :: bar, barbar

      allocate( character(len=*) :: bar , barbar) ! <= Here!

   end subroutine

end
