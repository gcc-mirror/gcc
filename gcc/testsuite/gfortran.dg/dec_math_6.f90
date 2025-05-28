! { dg-options "-std=f2018" }
! { dg-do compile }

intrinsic :: acospi ! { dg-error "The intrinsic 'acospi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: asinpi ! { dg-error "The intrinsic 'asinpi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: atanpi ! { dg-error "The intrinsic 'atanpi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: atan2pi ! { dg-error "The intrinsic 'atan2pi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: cospi ! { dg-error "The intrinsic 'cospi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: sinpi ! { dg-error "The intrinsic 'sinpi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }
intrinsic :: tanpi ! { dg-error "The intrinsic 'tanpi' declared INTRINSIC at .1. is not available in the current standard settings but new in Fortran 2023. Use an appropriate '-std=\\*' option or enable '-fall-intrinsics' in order to use it" }

end
