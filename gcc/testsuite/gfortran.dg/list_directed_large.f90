! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! PR libfortran/89274 Inconsistent list directed output of INTEGER(16)
!
integer(16) :: j(2)
character(82) :: str
j = huge(1_16)
write(str,*) j
if (str /= "  170141183460469231731687303715884105727  170141183460469231731687303715884105727") stop 1
j = 1
write(str,*) j
if (str /= "                                        1                                        1") stop 2
j = -huge(1_16)
write(str,*) j
if (str /= " -170141183460469231731687303715884105727 -170141183460469231731687303715884105727") stop 3
end
