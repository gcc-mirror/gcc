/* { dg-do assemble } */
/* { dg-options "-Os -fomit-frame-pointer -m68040" } */
/* { dg-final { object-size text <= 30 } } */

struct kobject {
        unsigned int b7:1;
        unsigned int b56:2;
        unsigned int b1234:4;
        unsigned int b0:1;
        unsigned char x;
        unsigned int f;
};

void b7(struct kobject *kobj)
{
        kobj->b7 = 1;
}

void b56(struct kobject *kobj)
{
        kobj->b56 = 3;
}

void b1234(struct kobject *kobj)
{
        kobj->b1234 = 15;
}
