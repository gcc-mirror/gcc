/* { dg-do compile } */

extern void *memcpy (void *__dest, __const void *__src, __SIZE_TYPE__  __n);

char internal_crash_read_ip[] = { 0xb8 };

struct u_internal_crash_read_t
{
  char ip[sizeof (internal_crash_read_ip)];
}
u_internal_crash_read;

void
gSignalHandler (int psignalNr, int pinfo, int pctx)
{
  memcpy (u_internal_crash_read.ip, internal_crash_read_ip,
	  sizeof (internal_crash_read_ip));
}
