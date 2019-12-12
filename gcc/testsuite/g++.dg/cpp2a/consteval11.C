// { dg-do compile }
// { dg-options "-std=c++2a" }

consteval int bar (int i) { if (i != 1) throw 1; return 0; }	// { dg-error "is not a constant expression" }

constexpr int a = bar (1);
constexpr int b = bar (2);		// { dg-message "in 'constexpr' expansion of" }
constexpr int c = 0 ? bar (3) : 1;	// { dg-message "in 'constexpr' expansion of" }
const int d = bar (4);			// { dg-message "in 'constexpr' expansion of" }
const int e = 0 ? bar (5) : 1;		// { dg-message "in 'constexpr' expansion of" }
int f = bar (1);
int g = bar (6);			// { dg-message "in 'constexpr' expansion of" }
int h = 0 ? bar (7) : 1;		// { dg-message "in 'constexpr' expansion of" }

void
foo ()
{
  constexpr int a = bar (1);
  constexpr int b = bar (2);		// { dg-message "in 'constexpr' expansion of" }
  constexpr int c = 0 ? bar (3) : 1;	// { dg-message "in 'constexpr' expansion of" }
  const int d = bar (4);		// { dg-message "in 'constexpr' expansion of" }
  const int e = 0 ? bar (5) : 1;	// { dg-message "in 'constexpr' expansion of" }
  int f = bar (1);
  int g = bar (6);			// { dg-message "in 'constexpr' expansion of" }
  int h = 0 ? bar (7) : 1;		// { dg-message "in 'constexpr' expansion of" }
  h += 0 ? bar (8) : 1;			// { dg-message "in 'constexpr' expansion of" }
  if (0)
    bar (9);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (10);				// { dg-message "in 'constexpr' expansion of" }
  if (1)
    bar (11);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (12);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (0)
    bar (13);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (14);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (1)
    bar (15);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (16);				// { dg-message "in 'constexpr' expansion of" }
}

consteval int
baz ()
{
  constexpr int a = bar (1);
  constexpr int b = bar (2);		// { dg-message "in 'constexpr' expansion of" }
  constexpr int c = 0 ? bar (3) : 1;
  const int d = bar (4);
  const int e = 0 ? bar (5) : 1;
  int f = bar (1);
  int g = bar (6);
  int h = 0 ? bar (7) : 1;
  h += 0 ? bar (8) : 1;
  if (0)
    bar (9);
  else
    bar (10);
  if (1)
    bar (11);
  else
    bar (12);
  if constexpr (0)
    bar (13);
  else
    bar (14);
  if constexpr (1)
    bar (15);
  else
    bar (16);
  return 0;
}

template <typename T>
void
qux ()
{
  if (0)
    bar (2);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (3);				// { dg-message "in 'constexpr' expansion of" }
  if (1)
    bar (4);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (5);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (0)
    bar (6);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (7);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (1)
    bar (8);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar (9);				// { dg-message "in 'constexpr' expansion of" }
  if (0)
    bar ((T) 2);
  else
    bar ((T) 3);
  if (1)
    bar ((T) 4);
  else
    bar ((T) 5);
  if constexpr (0)
    bar ((T) 6);
  else
    bar ((T) 7);
  if constexpr (1)
    bar ((T) 8);
  else
    bar ((T) 9);
}

template <typename T>
void
quux ()
{
  if (0)
    bar ((T) 2);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar ((T) 3);				// { dg-message "in 'constexpr' expansion of" }
  if (1)
    bar ((T) 4);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar ((T) 5);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (0)
    bar ((T) 6);
  else
    bar ((T) 7);				// { dg-message "in 'constexpr' expansion of" }
  if constexpr (1)
    bar ((T) 8);				// { dg-message "in 'constexpr' expansion of" }
  else
    bar ((T) 9);
}

void
corge ()
{
  quux <int> ();
}
