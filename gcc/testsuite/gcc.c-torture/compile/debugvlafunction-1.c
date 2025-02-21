extern void dynreplace_trampoline(void);
extern void dynreplace_trampoline_endlabel(void);
int dynreplace_add_trampoline(void)
{
  unsigned long trampoline_code[(((unsigned long)
(&(dynreplace_trampoline_endlabel))
-(unsigned long) (&dynreplace_trampoline)))
];
}


