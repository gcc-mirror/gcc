/* { dg-do compile } */
/* { dg-options "-Wformat -Wformat-signedness" } */

__attribute__((format(printf, 3, 4)))
void dev_printk(const char *level, void *dev, const char *fmt, ...);

#define lpfc_vport_param_init(attr)	\
void lpfc_##attr##_init(void *vport, unsigned int val) \
{ \
	dev_printk("3", (void *)0, \
		   "0423 lpfc_"#attr" attribute cannot be set to %d, "\
		   "allowed range is [0, 1]\n", val); \
}

#define LPFC_VPORT_ATTR_R(name, desc) \
unsigned int lpfc_##name;\
lpfc_vport_param_init(name)\

LPFC_VPORT_ATTR_R(peer_port_login,
		  "Allow peer ports on the same physical port to login to each "
		  "other.");

/* { dg-warning "20: format .%d. expects argument of type .int., but argument 4 has type .unsigned int. " "" { target *-*-* } .-12 } */
