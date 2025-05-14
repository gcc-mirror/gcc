/* { dg-do "compile" } */
/* { dg-options "-std=gnu23" } */

typedef struct q { int x; } q_t;
struct q { int x; };
typedef struct q { int x; } q_t;

