! { dg-do run }
!
! Test the fix for PR92959, where compilation of ASSOCIATED segfaulted in 's1' and 's2'.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   character(:), pointer :: x, y => NULL()
   character, pointer :: u, v => NULL ()
   character(4), target :: tgt = "abcd"

! Manifestly not associated
   x => tgt
   u => tgt(1:1)
   call s1 (.false., 1)
   call s2 (.false., 2)
! Manifestly associated
   y => x
   v => u
   call s1 (.true., 3)
   call s2 (.true., 4)
! Zero sized storage sequences must give a false.
   y => tgt(1:0)
   x => y
   call s1 (.false., 5)
contains
   subroutine s1 (state, err_no)
      logical :: state
      integer :: err_no
      if (associated(x, y) .neqv. state) stop err_no
   end
   subroutine s2 (state, err_no)
      logical :: state
      integer :: err_no
      if (associated(u, v) .neqv. state) stop err_no
    end
end
