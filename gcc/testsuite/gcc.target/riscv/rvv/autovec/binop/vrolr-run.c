/* { dg-do run } */
/* { dg-require-effective-target "riscv_zvbb_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include <stdint-gcc.h>
#include <assert.h>

#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#define ARRAY_SIZE 512

#define CIRCULAR_LEFT_SHIFT_ARRAY(arr, shifts, bit_size, size) \
    for (int i = 0; i < size; i++) { \
        (arr)[i] = (((arr)[i] << (shifts)[i]) | ((arr)[i] >> (bit_size - (shifts)[i]))); \
    }

#define CIRCULAR_RIGHT_SHIFT_ARRAY(arr, shifts, bit_size, size) \
    for (int i = 0; i < size; i++) { \
        (arr)[i] = (((arr)[i] >> (shifts)[i]) | ((arr)[i] << (bit_size - (shifts)[i]))); \
    }

void __attribute__((optimize("no-tree-vectorize"))) compare_results8(
    uint8_t *result_left, uint8_t *result_right,
    int bit_size, uint8_t *shift_values)
{
    for (int i = 0; i < ARRAY_SIZE; i++) {
        assert(result_left[i] == (i << shift_values[i]) | (i >> (bit_size - shift_values[i])));
        assert(result_right[i] == (i >> shift_values[i]) | (i << (bit_size - shift_values[i])));
    }
}

void __attribute__((optimize("no-tree-vectorize"))) compare_results16(
    uint16_t *result_left, uint16_t *result_right,
    int bit_size, uint16_t *shift_values)
{
    for (int i = 0; i < ARRAY_SIZE; i++) {
        assert(result_left[i] == (i << shift_values[i]) | (i >> (bit_size - shift_values[i])));
        assert(result_right[i] == (i >> shift_values[i]) | (i << (bit_size - shift_values[i])));
    }
}

void __attribute__((optimize("no-tree-vectorize"))) compare_results32(
    uint32_t *result_left, uint32_t *result_right,
    int bit_size, uint32_t *shift_values)
{
    for (int i = 0; i < ARRAY_SIZE; i++) {
        assert(result_left[i] == (i << shift_values[i]) | (i >> (bit_size - shift_values[i])));
        assert(result_right[i] == (i >> shift_values[i]) | (i << (bit_size - shift_values[i])));
    }
}

void __attribute__((optimize("no-tree-vectorize"))) compare_results64(
    uint64_t *result_left, uint64_t *result_right,
    int bit_size, uint64_t *shift_values)
{
    for (int i = 0; i < ARRAY_SIZE; i++) {
        assert(result_left[i] == ((uint64_t)i << shift_values[i]) | ((uint64_t)i >> (bit_size - shift_values[i])));
        assert(result_right[i] == ((uint64_t)i >> shift_values[i]) | ((uint64_t)i << (bit_size - shift_values[i])));
    }
}

#define TEST_SHIFT_OPERATIONS(TYPE, bit_size) \
    TYPE shift_val##bit_size[ARRAY_SIZE];\
    TYPE result_left##bit_size[ARRAY_SIZE];\
    TYPE result_right##bit_size[ARRAY_SIZE];\
    do { \
        for (int i = 0; i < ARRAY_SIZE; i++) { \
	    result_left##bit_size[i] = i;\
	    result_right##bit_size[i] = i;\
            shift_val##bit_size[i] = i % bit_size; \
        } \
	CIRCULAR_LEFT_SHIFT_ARRAY(result_left##bit_size, shift_val##bit_size, bit_size, ARRAY_SIZE)\
	CIRCULAR_RIGHT_SHIFT_ARRAY(result_right##bit_size, shift_val##bit_size, bit_size, ARRAY_SIZE)\
        compare_results##bit_size(result_left##bit_size, result_right##bit_size, bit_size, shift_val##bit_size); \
    } while(0)


int main() {
    TEST_SHIFT_OPERATIONS(uint8_t, 8);
    TEST_SHIFT_OPERATIONS(uint16_t, 16);
    TEST_SHIFT_OPERATIONS(uint32_t, 32);
    TEST_SHIFT_OPERATIONS(uint64_t, 64);
    return 0;
}
