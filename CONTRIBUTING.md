## How to contribute to GCC Rust

#### **Did you find a bug?**

* **Ensure the bug was not already reported** by searching on GitHub under [Issues](https://github.com/Rust-GCC/gccrs/issues).

* If you're unable to find an open issue addressing the problem, [open a new one](https://github.com/Rust-GCC/gccrs/issues/new).
  Be sure to include a **title and clear description**, as much relevant information as possible, and a **code sample**
  or an **executable test case** demonstrating the expected behavior that is not occurring.

#### **Do you want to submit a patch?**

* Open a new GitHub pull request with the patch.

* Ensure the PR description clearly describes the problem and solution. Include the relevant issue number if applicable.

* Before submitting, GCC development requires copyright assignment or the Developer's Certificate of Origin sign-off.
   Please see the [Contributing to GCC](https://gcc.gnu.org/contribute.html) guide or [Developer's Certificate of Origin (DCO) Sign-off](https://gcc.gnu.org/dco.html) guide.

* Patches sent to the [`gcc-rust` mailing list](https://gcc.gnu.org/mailman/listinfo/gcc-rust) are likewise welcome.
These will be imported into a GitHub PR to follow the normal review process,
and the link to the GitHub PR sent to the submitter.

#### **Do you intend to add a new feature or change an existing one?**

* Suggest your change in the [Zulip](https://gcc-rust.zulipchat.com/) and start writing code.

* Do not open an issue on GitHub until you have collected positive feedback about the change.
  GitHub issues are primarily intended for bug reports and fixes.

#### **Do you have questions about the source code?**

* Ask any question about how to use GCCRS in [Zulip](https://gcc-rust.zulipchat.com/).

### **PR Policy**

* The PR policy: Everything has to go through a PR
  - An exception to this rule will be the merge commits of updating the repo against upstream GCC

* Reviewers/Maintainers of the project (aka people who have bors rights) should be pinged for reviews/questions.

* A PR can have one or several commits (split should have a technical/logical reason, ie. no fixup-ish commit)

* Avoid PR's with merge commit unless there's a good reason

* Where possible please add test cases to `gcc/testsuite/rust/` for all PRs.
  Some issues may not be testable via dejagnu/automation such as debug dump changes.

* Follow the [GCC coding style](https://gcc.gnu.org/codingconventions.html) (see `clang-format` below).

* PRs won't be merged until the build and tests pass.

* Please take the time to create good git commit messages.
  See the existing format of them in the git log or refer to something like: https://chris.beams.io/posts/git-commit/

#### Running `clang-format` locally

* on all files using python scripts
... corresponding to what the _Clang Format Lint_ (`.github/workflows/clang-format.yml`)
is doing, with `clang-format-10` being available locally, and avoiding the Docker overhead.

```shell
$ wget 'https://github.com/DoozyX/clang-format-lint-action/raw/v0.11/run-clang-format.py'
$ cp contrib/clang-format .clang-format
$ python3 run-clang-format.py --clang-format-executable clang-format-10 --recursive --extensions h,cc gcc/rust/
```

* on a given patch using python scripts
See the [clang-format documentation](https://clang.llvm.org/docs/ClangFormat.html#script-for-patch-reformatting) :

    $ git diff -U0 --no-color HEAD^ | clang-format-diff.py -i -p1

* using `git` interface

At least on Debian and its derivative, each `clang-format` packages also comes
with `git-clang-format` command that can be used easily. It applies on staged
changes, and any modification can be seen as unstaged changes:

```diff
$ git diff --cached
diff --git a/gcc/rust/rust-abi.h b/gcc/rust/rust-abi.h
index bd3043295ce..9559374ce60 100644
--- a/gcc/rust/rust-abi.h
+++ b/gcc/rust/rust-abi.h
@@ -22,10 +22,10 @@ namespace Rust {
 enum ABI
 {
   UNKNOWN,
-  RUST,
+     RUST,
   INTRINSIC,
   C,
-  CDECL,
+     CDECL,
   STDCALL,
   FASTCALL,
 };

gccrs/gcc/rust on  dkm/clang_format [$!+?]
❯ git clang-format
changed files:
    gcc/rust/rust-abi.h

gccrs/gcc/rust on  dkm/clang_format [$!+?]
$ git diff rust-abi.h
diff --git a/gcc/rust/rust-abi.h b/gcc/rust/rust-abi.h
index 9559374ce60..bd3043295ce 100644
--- a/gcc/rust/rust-abi.h
+++ b/gcc/rust/rust-abi.h
@@ -22,10 +22,10 @@ namespace Rust {
 enum ABI
 {
   UNKNOWN,
-     RUST,
+  RUST,
   INTRINSIC,
   C,
-     CDECL,
+  CDECL,
   STDCALL,
   FASTCALL,
 };
```

Also note that you can use a given version of `clang-format` by using `git clang-format-10` if you have
installed that particular version.

Thanks! :heart: :heart: :heart:

GCCRS Team
