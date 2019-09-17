/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v5t_thumb_ok } */
/* { dg-options "-mfloat-abi=softfp -mthumb -march=armv5t -O2" }  */
#pragma GCC optimize ("-O2")

struct itrm {
 int std_in;
 int std_out;
 int sock_in;
 int sock_out;
};

struct itrm *alloc(void);
void queue_event(struct itrm *itrm, unsigned char *data, int len);

void handle_trm(int std_in, int std_out, int sock_in, int sock_out, int ctl_in, void *init_string, int init_len)
{
 struct itrm *itrm;
 struct itrm ev = { 0, 80, 24, 0 };
 itrm = alloc();
 itrm->std_in = std_in;
 itrm->std_out = std_out;
 itrm->sock_in = sock_in;
 itrm->sock_out = sock_out;
 queue_event(itrm, (unsigned char *)&ev, sizeof(struct itrm));
}
