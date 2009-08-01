! { dg-do compile }
! { dg-options "-fwhole-file" }
! Test the fix for the fifth problem in PR40011, where the
! entries were not resolved, resulting in a segfault.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
recursive function fac(i) result (res)
  integer :: i, j, k, res
  k = 1
  goto 100
entry bifac(i,j) result (res)
  k = j
100 continue
  if (i < k) then
    res = 1
  else
    res = i * bifac(i-k,k)
  end if
end function

program test
  external fac
  external bifac
  integer :: fac, bifac
  print *, fac(5)
  print *, bifac(5,2)
  print*, fac(6)
  print *, bifac(6,2)
  print*, fac(0)
  print *, bifac(1,2)
end program test
