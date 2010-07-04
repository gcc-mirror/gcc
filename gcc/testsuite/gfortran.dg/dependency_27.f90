! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 36928 - optimize array interleaving array temporaries
program main
  real, dimension(20) :: a
  read (10) a
  a(2:10:2) = a (1:9:2)
  write (11) a
  read (10) a
  a(2:10:4) = a(1:5:2)
  write (11) a
  read (10) a
  a(2:10:4) = a(5:1:-2)
  write (11) a
end program main
