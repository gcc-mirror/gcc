! PR fortran/98476

subroutine abc(cc)
    integer, target :: cc, dd
    cc = 131
    dd = 484

    !$omp target enter data map(to: cc, dd)

    !$omp target data use_device_addr(cc) use_device_ptr(dd)
      !$omp target is_device_ptr(cc, dd)  ! Valid since OpenMP 5.1
        if (cc /= 131 .or. dd /= 484) stop 1
        cc = 44
        dd = 45
      !$omp end target
    !$omp end target data

    !$omp target exit data map(from:cc, dd)

    if (cc /= 44 .or. dd /= 45) stop 5
end
