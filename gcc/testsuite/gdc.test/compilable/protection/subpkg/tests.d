module protection.subpkg.tests;

import crosspkg = protection.basic.mod1;

static assert ( is(typeof(crosspkg.publicFoo())));
static assert (!is(typeof(crosspkg.packageFoo())));
static assert (!is(typeof(crosspkg.privateFoo())));

import samepkg = protection.subpkg.explicit;

static assert ( is(typeof(samepkg.commonAncestorFoo())));
static assert ( is(typeof(samepkg.samePkgFoo())));
