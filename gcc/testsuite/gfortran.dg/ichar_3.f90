! { dg-do compile }
!
! PR fortran/59599
! The call to ichar was triggering an ICE.
!
! Original testcase from Fran Martinez Fadrique <fmartinez@gmv.com>

character(1) cpk(2)
integer res(2)
cpk = 'a'
res = ichar( cpk, kind=1 )
print *, ichar( cpk, kind=1 )
end
