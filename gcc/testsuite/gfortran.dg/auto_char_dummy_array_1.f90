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

  nullify(ptr)

  call a (ptr, 12)
  if (.not.associated (ptr) ) STOP 1
  if (any (ptr.ne."abc")) STOP 2

  ptr => null ()              ! ptr points to 't' here.
  allocate (ptr(3))
  ptr = "xyz"
  call a (ptr, 12)

  if (.not.associated (ptr)) STOP 3
  if (any (ptr.ne."lmn")) STOP 4

  call a (ptr, 0)

  if (associated (ptr)) STOP 5

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
      if (size (p,1).ne.3) STOP 6
      if (any (p.ne."xyz")) STOP 7
      p = s
    end if
  end subroutine a

end program oh_no_not_pr15908_again
