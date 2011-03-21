! { dg-do run }
! PR47154 END= does not work in namelist read
      program foo
      real :: a
      namelist /b/a
      open(10,status="scratch")
      read (10,nml=b,end=100)
 100  continue
      end
