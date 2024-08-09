!{ dg-do run }

! Check PR 116292 is fixed.

! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!                Sam James  <sjames@gcc.gnu.org>

program move_alloc_19
  character, allocatable :: buffer, dummy, dummy2
  class(*), allocatable :: poly

  dummy = 'C'
  dummy2 = 'A'
  call s()
  if (allocated (dummy)) stop 1
  if (allocated (dummy2)) stop 2
  if (.not. allocated (buffer)) stop 3
  if (.not. allocated (poly)) stop 4
  if (buffer /= 'C') stop 5
  select type (poly)
    type is (character(*))
      if (poly /= 'A') stop 6
      if (len (poly) /= 1) stop 7
    class default
      stop 8
  end select
  deallocate (poly, buffer)
contains
  subroutine s
    call move_alloc (dummy, buffer)
    call move_alloc (dummy2, poly)
  end
end

