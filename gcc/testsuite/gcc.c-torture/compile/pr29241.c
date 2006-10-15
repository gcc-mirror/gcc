static inline __attribute__((always_inline)) void ip_finish_output2(){}
void ip_fragment(void (*)(void));
static inline __attribute__((always_inline)) void ip_finish_output()
{
 ip_fragment(ip_finish_output2);
 ip_finish_output2();
}
void ip_mc_output()
{
 ip_finish_output();
}
void ip_output()
{
 ip_finish_output();
}
