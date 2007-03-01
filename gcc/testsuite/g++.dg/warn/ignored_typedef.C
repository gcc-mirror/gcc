// PR c++/23689
// Test that malformed typedef's produce a compiler warning.

typedef char valid_0;
typedef int valid_1;
typedef long valid_2;
typedef float valid_3;
typedef double valid_4;
typedef unsigned valid_5;
typedef int *valid_6;
typedef struct valid_7 {} valid_8;
typedef struct {} valid_9;
typedef int temp_0; typedef temp_0 valid_10;
struct temp_1 {}; typedef temp_1 valid_11;
union temp_2 {}; typedef temp_2 valid_12;
typedef void (*valid_13) (int); 

typedef struct invalid_0 {};  // { dg-warning "'typedef' was ignored" }
typedef class invalid_1 {};  // { dg-warning "'typedef' was ignored" }
typedef enum invalid_2 { INVALID_2 };  // { dg-warning "'typedef' was ignored" }
typedef enum { INVALID_3 };  // { dg-warning "'typedef' was ignored" }
typedef union invalid_4 {};  // { dg-warning "'typedef' was ignored" }
