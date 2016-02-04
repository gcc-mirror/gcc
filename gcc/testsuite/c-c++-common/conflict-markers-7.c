/* It's valid to stringize the "<<<<<<<"; don't
   report it as a conflict marker.  */
#define str(s) #s
const char *s = str(
<<<<<<<
);
