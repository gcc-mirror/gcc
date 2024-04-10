/* { dg-do compile } */
/* { dg-options "-gdwarf-3 -dA" } */

struct foo {
        int field_number_1;
        int field_number_2;
        int field_number_3;
        int field_number_4;
        int field_number_5;
};

typedef int fun_t(struct foo *restrict);

int main() {
        return 0;
}

/* { dg-final { scan-assembler-not "DW_TAG_structure_type" } } */
