// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 May 2005 <nathan@codesourcery.com>

// PR 21165. ICE on valid
// Origin:Volker Reichelt  reichelt@gcc.gnu.org

template<typename T> bool foo()
{
    const int i = T();
    return i > 0;
}
