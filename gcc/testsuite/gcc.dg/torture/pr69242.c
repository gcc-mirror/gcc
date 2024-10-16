/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17 -w" } */

int a[1];
void *memcpy();
int smx_ctx_base_factory_create_context_sized();
void getcontext();
void smx_ctx_sysv_create_context() {
    int *b = (int *)smx_ctx_base_factory_create_context_sized();
    getcontext();
    memcpy(a, &b, sizeof(int));
    switch (a[0])
      ;
}

