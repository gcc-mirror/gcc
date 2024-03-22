#include <string.h>

void validatedatetime(const char *str)
{
  const char *templates[] = {"dddd-dd-dd dd:dd", "dddd-dd-dd"};

  size_t len = strlen(str);

  for (unsigned t = 0; t < 2; t++) {
    if (len != strlen(templates[t])) {
      continue;
    }
  }
}
