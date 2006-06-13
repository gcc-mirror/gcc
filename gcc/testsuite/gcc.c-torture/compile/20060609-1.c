/* This test used to ICE on IA64.  */
int __strspn_c2 (__const char *__s, int __accept1, int __accept2)
{
  register long unsigned int __result = 0;
  while (__s[__result] == __accept1 || __s[__result] == __accept2)
  return __result;
}
