/* { dg-do assemble } */
/* { dg-options "-Os -fomit-frame-pointer -m68040" } */
/* { dg-final { object-size text <= 72 } } */

struct kobject {
        unsigned int b7:1;
        unsigned int :6;
        unsigned int b0:1;
        unsigned char x;
        unsigned int f;
};

void ior(struct kobject *kobj) { kobj->f |= 4; }
void ior_m(struct kobject *kobj) { kobj->f |= -4; }

void xor(struct kobject *kobj) { kobj->f ^= 4; }
void xor_m(struct kobject *kobj) { kobj->f ^= -4; }

void and(struct kobject *kobj) { kobj->f &= 4; }
void and_m(struct kobject *kobj) { kobj->f &= -4; }
