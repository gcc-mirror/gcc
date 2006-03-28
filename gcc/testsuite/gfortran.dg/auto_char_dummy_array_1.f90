! { dg-do run }
! This tests the fix for pr15809 in which automatic character length,
! dummy, pointer arrays were broken.
!
! contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module global
  character(12), dimension(2), target :: t
end module global

program oh_no_not_pr15908_again
  character(12), dimension(:), pointer :: ptr

  call a (ptr, 12)
  if (.not.associated (ptr) ) call abort ()
  if (any (ptr.ne."abc")) call abort ()

  ptr => null ()              ! ptr points to 't' here.
  allocate (ptr(3))
  ptr = "xyz"
  call a (ptr, 12)

  if (.not.associated (ptr)) call abort ()
  if (any (ptr.ne."lmn")) call abort ()

  call a (ptr, 0)

  if (associated (ptr)) call abort ()

contains

  subroutine a (p, l)
    use global
    character(l), dimension(:), pointer :: p
    character(l), dimension(3)          :: s

    s = "lmn"

    if (l.ne.12) then
      deallocate (p)           ! ptr was allocated in main.
      p => null ()
      return
    end if

    if (.not.associated (p)) then
      t = "abc"
      p => t
    else
      if (size (p,1).ne.3) call abort ()
      if (any (p.ne."xyz")) call abort ()
      p = s
    end if
  end subroutine a

end program oh_no_not_pr15908_again

! { dg-final { cleanup-modules "global" } }
