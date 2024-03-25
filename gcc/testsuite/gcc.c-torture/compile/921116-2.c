/* { dg-additional-options "-std=gnu89" } */

typedef struct {
 long l[5];
} t;

f(size)
{
 t event;
 g(&(event.l[2 + size]), (3 - size) * 4);
}
