/* PR middle-end/60556 */

int g (int);

unsigned long long f (void)
{
 return (unsigned long long)(long)&g;
}
