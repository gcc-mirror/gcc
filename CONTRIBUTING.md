## How to contribute to GCC Rust

#### **Did you find a bug?**

* **Ensure the bug was not already reported** by searching on GitHub under [Issues](https://github.com/Rust-GCC/gccrs/issues).

* If you're unable to find an open issue addressing the problem, [open a new one](https://github.com/Rust-GCC/gccrs/issues/new). Be sure to include a **title and clear description**, as much relevant information as possible, and a **code sample** or an **executable test case** demonstrating the expected behavior that is not occurring.

#### **Did you write a patch that fixes a bug?**

* Open a new GitHub pull request with the patch.

* Ensure the PR description clearly describes the problem and solution. Include the relevant issue number if applicable.

* Before submitting, GCCRS requires copyright assignment. Please read the [Contributing to GCC](https://gcc.gnu.org/contribute.html) guide to know more.

#### **Do you intend to add a new feature or change an existing one?**

* Suggest your change in the [Zulip](https://gcc-rust.zulipchat.com/) and start writing code.

* Do not open an issue on GitHub until you have collected positive feedback about the change. GitHub issues are primarily intended for bug reports and fixes.

#### **Do you have questions about the source code?**

* Ask any question about how to use GCCRS in [Zulip](https://gcc-rust.zulipchat.com/).

### **PR Policy**

* The PR policy: Everything has to go through a PR
  - An exception to this rule will be the merge commits of updating the repo against upstream GCC

* Reviewers/Maintainers of the project (aka people who have bors rights) should be pinged for reviews/questions.

* A PR can have one or several commits (split should have a technical/logical reason, ie. no fixup-ish commit)

* Avoid PR's with merge commit unless there's a good reason

* Where possible please add test cases to `gcc/testsuite/rust/` for all PRs. Some issues may not be testable via dejagnu/automation such as debug dump changes.

* PR's cannot be merged untill clang format and the build and tests pass.

* Please take the time to create good git commit messages see the existing format of them in the git log or refer to something like: https://chris.beams.io/posts/git-commit/

Thanks! :heart: :heart: :heart:

GCCRS Team
