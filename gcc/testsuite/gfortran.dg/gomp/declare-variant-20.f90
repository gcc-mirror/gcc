! PR middle-end/113904

module m
  implicit none (type, external)
  logical, parameter :: parameter_true = .false.
  logical :: false_flag = .false.
  integer :: my_dev_num
contains
  integer function variant1() result(res)
    res = 1
  end function

  integer function variant2() result(res)
    res = 2
  end function

  integer function variant3() result(res)
    res = 3
  end function

  integer function variant4() result(res)
    res = 4
  end function

  integer function variant5() result(res)
    res = 4
  end function

  integer function variant6() result(res)
    res = 4
  end function

  integer function foo() result(res)
    ! 'condition'
    !$omp  declare variant(variant1) match(user={condition(parameter_true)},construct={teams})  ! OK
    ! Below: OK since OpenMP 5.1 - but not yet supported: PR middle-end/113904
    !$omp  declare variant(variant2) match(user={condition(false_flag)},construct={parallel})   ! { dg-error "property must be a constant logical expression" }
    !$omp  declare variant(variant3) match(user={condition(1)},construct={target})              ! { dg-error "property must be a constant logical expression" }

    ! 'device_num'
    !$omp  declare variant(variant4) match(target_device={device_num(0)})   ! OK
    !$omp  declare variant(variant4) match(target_device={device_num(2)})   ! OK - assuming there are two non-host devices.
    !$omp  declare variant(variant5) match(target_device={device_num(-1)})  ! OK - omp_initial_device
    !$omp  declare variant(variant5) match(target_device={device_num(-4)})  ! OK - omp_invalid_device (will never match)
    ! OK - but not handled -> PR middle-end/113904
    !$omp  declare variant(variant5) match(target_device={device_num(my_device)}) ! { dg-error "property must be a constant integer expression" }
    !$omp  declare variant(variant5) match(target_device={device_num(-2)})  ! { dg-error "property must be a conforming device number" }

    res = 99
  end
end module m
