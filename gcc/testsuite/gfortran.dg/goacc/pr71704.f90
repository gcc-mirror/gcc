! PR fortran/71704
! { dg-do compile }

real function f1 ()
!$acc routine (f1)
  f1 = 1
end

real function f2 (a)
  integer a
  !$acc enter data copyin(a)
  f2 = 1
end

real function f3 (a)
  integer a
!$acc enter data copyin(a)
  f3 = 1
end

real function f4 ()
!$acc wait
  f4 = 1
end

real function f5 (a)
  integer a
!$acc update device(a)
  f5 = 1
end

real function f6 ()
!$acc parallel
!$acc end parallel
  f6 = 1
end

real function f7 ()
!$acc kernels
!$acc end kernels
  f7 = 1
end

real function f_serial ()
!$acc serial
!$acc end serial
  f_serial = 1
end

real function f8 ()
!$acc data
!$acc end data
  f8 = 1
end

real function f9 (a)
  integer a(:)
!$acc host_data use_device(a)
!$acc end host_data
  f8 = 1
end

real function f10 (a)
  integer a
!$acc declare present (a)
  f8 = 1
end
