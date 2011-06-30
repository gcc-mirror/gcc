! PR fortran/49540
! { dg-do compile }
! { dg-options "" }
block data
  common /a/ i(5,5)
  data i /4, 23 * 5, 6/
  data i(:,2) /1, 3 * 2, 3/
  common /b/ j(5,5)
  data j(2,:) /1, 3 * 2, 3/
  data j /4, 23 * 5, 6/
  common /c/ k(5,5)
  data k(:,2) /1, 3 * 2, 3/
  data k /4, 23 * 5, 6/
  common /d/ l(5,5)
  data l /4, 23 * 5, 6/
  data l(2,:) /1, 3 * 2, 3/
end block data
