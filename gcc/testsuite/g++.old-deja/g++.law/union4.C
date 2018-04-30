// { dg-do assemble  }
// GROUPS passed unions
// anon-union file
// From: hossein@veritas.com (Hossein Raassi)
// Date:     Wed, 15 Dec 93 13:52 PST
// Subject:  Internal Error
// Message-ID: <m0pA49A-0000LdC@piano.veritas.com>

static union {
        struct SS {
                int ss;
        };
};// { dg-error "no members" }
