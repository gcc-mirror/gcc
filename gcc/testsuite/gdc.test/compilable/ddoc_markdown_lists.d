// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_lists.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_lists.html

/++
# Lists

## Unordered

- item one

  *part of* item one

  ---
  // code in item one
  ---

 **not** part of item one

+ + three
- different
* lists

- list with
-
- empty item

- parent item
  - child item

- sibling item
 - sibling item

After text:
- ### heading
- and item

## Ordered

0. zero
1. one

List separator text

3. list that starts with three

1. parent item
   1. child item

1. sibling item
  2. sibling item

## Not Lists

-no initial space

2.no initial space

1234567890. too many numbers

-1. negative

New lists must start with 1, not
6. So this isn't a list.

+/
module ddoc_markdown_lists;
