/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -fdump-tree-optimized" } */

double pow_one_inf() { return __builtin_pow(1.0,__builtin_inf()); }
double pow_one_ninf() { return __builtin_pow(1.0,-__builtin_inf()); }
double pow_one_nan() { return __builtin_pow(1.0,__builtin_nan("")); }

double pow_inf_zero() { return __builtin_pow(__builtin_inf(),0.0); }
double pow_ninf_zero() { return __builtin_pow(-__builtin_inf(),0.0); }
double pow_nan_zero() { return __builtin_pow(__builtin_nan(""),0.0); }
double pow_inf_mzero() { return __builtin_pow(__builtin_inf(),-0.0); }
double pow_ninf_mzero() { return __builtin_pow(-__builtin_inf(),-0.0); }
double pow_nan_mzero() { return __builtin_pow(__builtin_nan(""),-0.0); }

double pow_mone_inf() { return __builtin_pow(-1.0,__builtin_inf()); }
double pow_mone_ninf() { return __builtin_pow(-1.0,__builtin_inf()); }

double pow_half_ninf() { return __builtin_pow(0.5,-__builtin_inf()); }
double pow_mhalf_ninf() { return __builtin_pow(-0.5,-__builtin_inf()); }
double pow_two_ninf() { return __builtin_pow(2.0,-__builtin_inf()); }
double pow_mtwo_ninf() { return __builtin_pow(-2.0,-__builtin_inf()); }
double pow_inf_ninf() { return __builtin_pow(__builtin_inf(),-__builtin_inf()); }
double pow_ninf_ninf() { return __builtin_pow(-__builtin_inf(),-__builtin_inf()); }

double pow_half_inf() { return __builtin_pow(0.5,__builtin_inf()); }
double pow_mhalf_inf() { return __builtin_pow(-0.5,__builtin_inf()); }
double pow_two_inf() { return __builtin_pow(2.0,__builtin_inf()); }
double pow_mtwo_inf() { return __builtin_pow(-2.0,__builtin_inf()); }
double pow_inf_inf() { return __builtin_pow(__builtin_inf(),__builtin_inf()); }
double pow_ninf_inf() { return __builtin_pow(-__builtin_inf(),__builtin_inf()); }

double pow_ninf_mthree() { return __builtin_pow(-__builtin_inf(),-3.0); }
double pow_ninf_mtwo() { return __builtin_pow(-__builtin_inf(),-2.0); }

double pow_ninf_three() { return __builtin_pow(-__builtin_inf(),3.0); }
double pow_ninf_two() { return __builtin_pow(-__builtin_inf(),2.0); }

double pow_inf_mtwo() { return __builtin_pow(__builtin_inf(),-2.0); }
double pow_inf_mone() { return __builtin_pow(__builtin_inf(),-1.0); }
double pow_inf_half() { return __builtin_pow(__builtin_inf(),0.5); }
double pow_inf_two() { return __builtin_pow(__builtin_inf(),2.0); }

/* { dg-final { scan-tree-dump-not "__builtin_pow" "optimized" } } */
