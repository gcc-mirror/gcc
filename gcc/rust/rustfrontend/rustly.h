#pragma once

#include "rust-linemap.h"
#include "rust-system.h"
#include "rust-c.h"

#include "rs-parser.h"
#include "node.h"

class Rustly
{
public:
    Rustly(bool only_check_syntax, Linemap* mLinemap);
    
    ~Rustly();

    void parse_input_files (size_t n, const char** in);

    void do_compile();

private:
    bool     mSyntaxOnly;
    Linemap* mLinemap;
};


