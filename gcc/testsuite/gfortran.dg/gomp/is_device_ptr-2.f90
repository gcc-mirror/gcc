! PR fortran/98476

subroutine abc(cc)
    integer, target :: cc, dd
    cc = 131
    dd = 484

    !$omp target enter data map(to: cc, dd)

    !$omp target data use_device_addr(cc) use_device_ptr(dd) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use USE_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
      !$omp target is_device_ptr(cc, dd) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use HAS_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
        if (cc /= 131 .or. dd /= 484) stop 1
        cc = 44
        dd = 45
      !$omp end target
    !$omp end target data

    !$omp target exit data map(from:cc, dd)

    if (cc /= 44 .or. dd /= 45) stop 5
end
