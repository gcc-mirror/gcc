/* { dg-do compile } */
/* { dg-options "-O2 -Wvector-operation-performance" } */
/* { dg-additional-options "-mno-sse" { target x86_64-*-* i?86-*-* } } */

enum { QEMU_MIGRATION_COOKIE_PERSISTENT = 1 };
struct {
  unsigned flags;
  unsigned flagsMandatory;
} qemuMigrationCookieGetPersistent_mig;
void qemuMigrationCookieGetPersistent()
{
  qemuMigrationCookieGetPersistent_mig.flags &=  /* { dg-bogus "will be expanded" } */
      QEMU_MIGRATION_COOKIE_PERSISTENT;
  qemuMigrationCookieGetPersistent_mig.flagsMandatory &=
      QEMU_MIGRATION_COOKIE_PERSISTENT;
}
