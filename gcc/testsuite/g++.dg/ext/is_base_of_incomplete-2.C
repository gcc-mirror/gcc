struct T;

int check1[__is_base_of(T, T) ? 1 : -1];
int check2[__is_base_of(T, const T) ? 1 : -1];
int check3[__is_base_of(volatile T, T) ? 1 : -1];
