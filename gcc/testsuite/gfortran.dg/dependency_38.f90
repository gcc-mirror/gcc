! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 45159 - No temporary should be created for this.
program main
  integer a(100)
  a(10:16:2) = a(10:16:2)
  a(10:16:2) = a(10:19:3)
  a(10:18:2) = a(12:20:2)
  a(1:10) = a(2:20:2)
  a(16:10:-2) = a(16:10:-2)
  a(19:10:-1) = a(19:1:-2)
  a(19:10:-1) = a(18:9:-1)
  a(19:11:-1) = a(18:2:-2)
end program main
