// PRMS Id: 5652
// Bug: strings does not get initialized.

extern "C" void * memcpy (void *, const void *, __SIZE_TYPE__);
extern "C" int strcmp (const char *, const char *);

class My_string {
   char *str;
   int len;
public:
   My_string(const char* string);
   My_string(const My_string &);
   ~My_string() { delete str; }
   char* char_p() { return str; }
};

const My_string strings[4] = {
   "first string",
   "second string",
   "third string",
   "fourth string"
};

My_string::My_string(const char* string)
{
   len = strlen(string) + 1;
   str = new char[len];
   memcpy(str, string, len);
}

My_string::My_string(const My_string &string)
{
   len = string.len;
   str = new char[len];
   memcpy(str, string.str, len);
}

int main()
{
   My_string str1 = strings[0];
   return strcmp ("first string", str1.char_p ()) != 0;
}
