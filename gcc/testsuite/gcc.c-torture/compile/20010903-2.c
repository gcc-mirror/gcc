extern int __dummy (void *__preg, const char *__string);
extern int rpmatch (const char *response);

int
rpmatch (const char *response)
{
  auto inline int try (void *re);

  inline int try (void *re)
    {
      return __dummy (re, response);
    }
  static void *yesre;
  return (try (&yesre));
}
