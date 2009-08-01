! { dg-do compile }
! { dg-options "-fwhole-file" }
! Test the fix for the fourth problem in PR40011, where the
! entries were not resolved, resulting in a segfault.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
program test
interface
  function bad_stuff(n)
    integer :: bad_stuff (2)
    integer :: n(2)
  end function bad_stuff
   recursive function rec_stuff(n) result (tmp)
    integer :: n(2), tmp(2)
  end function rec_stuff
end interface
   integer :: res(2)
  res = bad_stuff((/-19,-30/))

end program test

  recursive function bad_stuff(n)
    integer :: bad_stuff (2)
    integer :: n(2), tmp(2), ent = 0, sent = 0
    save ent, sent
    ent = -1
   entry rec_stuff(n) result (tmp)
    if (ent == -1) then
      sent = ent
      ent = 0
    end if
    ent = ent + 1
    tmp = 1
    if(maxval (n) < 5) then
      tmp = tmp + rec_stuff (n+1)
      ent = ent - 1
    endif
    if (ent == 1) then
      if (sent == -1) then
        bad_stuff = tmp + bad_stuff (1)
      end if
      ent = 0
      sent = 0
    end if
  end function bad_stuff
