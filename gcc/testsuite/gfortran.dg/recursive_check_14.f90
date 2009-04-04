! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR fortran/39577
!
! Recursive but valid program
! Contributed by Dominique Dhumieres
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
interface
  recursive function fac(n) result (res)
    integer :: res
    integer :: n
  end function fac
  recursive function bifac(m,n) result (res)
    integer :: m, n, res
  end function  bifac
end interface

  print *, fac(5)
  print *, bifac(5,2)
  print*, fac(6)
  print *, bifac(6,2)
  print*, fac(0)
  print *, bifac(1,2)
end program test
