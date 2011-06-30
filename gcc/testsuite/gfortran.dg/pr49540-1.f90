! PR fortran/49540
! { dg-do compile }
block data
  common /a/ b(100000,100)
  data b /10000000 * 0.0/
end block data
