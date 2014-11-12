#include "builtin-arith-overflow.h"

#define TESTS(type, min, max) \
ST (100, signed type, 2, 3, 5, U(s, add), 0) \
ST (101, signed type, max, -1, max - 1, U(s, add), 0) \
ST (102, signed type, max, 0, max, U(s, add), 0) \
ST (103, signed type, 1, max, min, U(s, add), 1) \
ST (104, signed type, 0, min, min, U(s, sub), 1) \
ST (110, signed type, 2, 3, -1, U(s, sub), 0) \
ST (111, signed type, max, -1, min, U(s, sub), 1) \
ST (112, signed type, max, 0, max, U(s, sub), 0) \
ST (113, signed type, 1, max, min + 2, U(s, sub), 0) \
ST (114, signed type, max, -1, min, U(s, sub), 1) \
ST (120, signed type, 2, 3, 6, U(s, mul), 0) \
ST (122, signed type, min, -1, min, U(s, mul), 1) \
ST (123, signed type, max, 0, 0, U(s, mul), 0) \
ST (124, signed type, 1, max, max, U(s, mul), 0) \
ST (125, signed type, max, 2, -2, U(s, mul), 1) \
ST (126, signed type, max / 25, 25, max / 25 * 25, U(s, mul), 0) \
ST (127, signed type, max / 25 + 1, 25, max / 25 * 25 + (unsigned type) 25, U(s, mul), 1) \
ST (150, unsigned type, 2, 3, 5, U(u, add), 0) \
ST (151, unsigned type, -1, -1, -2, U(u, add), 1) \
ST (152, unsigned type, -1, 0, -1, U(u, add), 0) \
ST (153, unsigned type, 1, -1, 0, U(u, add), 1) \
ST (154, unsigned type, 0, min, min, U(u, sub), 1) \
ST (160, unsigned type, 2, 3, -1, U(u, sub), 1) \
ST (161, unsigned type, -1, -1, 0, U(u, sub), 0) \
ST (162, unsigned type, -1, 0, -1, U(u, sub), 0) \
ST (163, unsigned type, 1, -1, 2, U(u, sub), 1) \
ST (164, unsigned type, 15, 14, 1, U(u, sub), 0) \
ST (170, unsigned type, 2, 3, 6, U(u, mul), 0) \
ST (171, unsigned type, max, 3, 3 * (unsigned type) max, U(u, mul), 1) \
ST (172, unsigned type, -1, 0, 0, U(u, mul), 0) \
ST (173, unsigned type, 1, -1, -1, U(u, mul), 0) \
ST (174, unsigned type, -1, 2, -2, U(u, mul), 1) \
ST (175, unsigned type, ((unsigned type) -1) / 25, 25, ((unsigned type) -1) / 25 * 25, U(u, mul), 0) \
ST (176, unsigned type, ((unsigned type) -1) / 25 + 1, 25, ((unsigned type) -1) / 25 * 25 + (unsigned type) 25, U(u, mul), 1)
