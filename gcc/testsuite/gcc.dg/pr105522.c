/* { dg-do compile { target *-*-darwin* } } */
typedef const struct __CFString * CFStringRef;

/* This used to ICE on powerpc darwin, in decode_addr_const.  */
const void
* create_usage_match(const unsigned int page, const unsigned int usage, int
*okay)
{

  const void *keys[2] = 
    { (void *) ((CFStringRef) __builtin___CFStringMakeConstantString (""
      "DeviceUsagePage" ""))
      , (void *) ((CFStringRef) __builtin___CFStringMakeConstantString (""
      "DeviceUsage" ""))
    };
  return keys[1];
}
