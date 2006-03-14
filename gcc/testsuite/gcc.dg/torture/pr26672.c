/* { dg-do compile } */

int printf(const char *format, ...);
extern const char help_string[];
void app_opts(void) {
    printf("%s", help_string);
}
const char help_string[] = "foo\n";
