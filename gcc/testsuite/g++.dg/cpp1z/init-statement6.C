// Testcase from P0305R1
// { dg-options -std=c++1z }

#include <string>
#include <map>
#include <algorithm>

std::map<int, std::string> m;
extern int xread (int *);
extern void publish (int), raise (int);

void
foo ()
{
  if (auto it = m.find (10); it != m.end ()) { std::string s = it->second; }
  if (char buf[10]; std::fgets(buf, 10, stdin)) { m[0] += buf; }
  if (int s; int count = xread (&s)) { publish(count); raise(s); }

  const char *s;
  if (auto keywords = {"if", "for", "while"};
      std::any_of(keywords.begin(), keywords.end(), [&s](const char* kw) { return s == kw; }))
    {
      // whatever
    }
}
