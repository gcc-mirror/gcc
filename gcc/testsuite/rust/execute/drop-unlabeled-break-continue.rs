// { dg-output "continue inner\r*\ncontinue outer\r*\ncontinue tail inner\r*\ncontinue tail outer\r*\nbreak inner\r*\nbreak outer\r*\nbreak tail inner\r*\nbreak tail outer\r*\nbreak value\r*\nvalue ok\r*\nwhile continue inner\r*\nwhile continue outer\r*\nwhile break inner\r*\nwhile break outer\r*\n" }
// { dg-additional-options "-w" }
#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

struct ContinueOuter;
struct ContinueInner;
struct ContinueTailOuter;
struct ContinueTailInner;
struct BreakOuter;
struct BreakInner;
struct BreakTailOuter;
struct BreakTailInner;
struct BreakValue;
struct WhileContinueOuter;
struct WhileContinueInner;
struct WhileBreakOuter;
struct WhileBreakInner;

impl Drop for ContinueOuter {
    fn drop(&mut self) {
        let msg = "continue outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for ContinueInner {
    fn drop(&mut self) {
        let msg = "continue inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for ContinueTailOuter {
    fn drop(&mut self) {
        let msg = "continue tail outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for ContinueTailInner {
    fn drop(&mut self) {
        let msg = "continue tail inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for BreakOuter {
    fn drop(&mut self) {
        let msg = "break outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for BreakInner {
    fn drop(&mut self) {
        let msg = "break inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for BreakTailOuter {
    fn drop(&mut self) {
        let msg = "break tail outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for BreakTailInner {
    fn drop(&mut self) {
        let msg = "break tail inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for BreakValue {
    fn drop(&mut self) {
        let msg = "break value\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for WhileContinueOuter {
    fn drop(&mut self) {
        let msg = "while continue outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for WhileContinueInner {
    fn drop(&mut self) {
        let msg = "while continue inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for WhileBreakOuter {
    fn drop(&mut self) {
        let msg = "while break outer\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

impl Drop for WhileBreakInner {
    fn drop(&mut self) {
        let msg = "while break inner\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

fn test_continue() {
    let mut done = false;

    loop {
        if done {
            break;
        }

        {
            let _outer = ContinueOuter;

            {
                let _inner = ContinueInner;
                done = true;
                continue;
            }
        }
    }
}

fn test_continue_tail_expr() {
    let mut done = false;

    loop {
        if done {
            break;
        }

        {
            let _outer = ContinueTailOuter;

            {
                let _inner = ContinueTailInner;
                done = true;
                continue
            }
        }
    }
}

fn test_break() {
    loop {
        let _outer = BreakOuter;

        {
            let _inner = BreakInner;
            break;
        }
    }
}

fn test_break_tail_expr() {
    loop {
        let _outer = BreakTailOuter;

        {
            let _inner = BreakTailInner;
            break
        }
    }
}

fn test_break_value() {
    let x = loop {
        let _value = BreakValue;
        break 123;
    };

    if x == 123 {
        let msg = "value ok\n\0" as *const str as *const i8;
        unsafe {
            printf(msg);
        }
    }
}

fn test_while_continue() {
    let mut i = 0;

    while i < 1 {
        let _outer = WhileContinueOuter;

        {
            let _inner = WhileContinueInner;
            i = i + 1;
            continue;
        }
    }
}

fn test_while_break() {
    let i = 0;

    while i < 1 {
        let _outer = WhileBreakOuter;

        {
            let _inner = WhileBreakInner;
            break;
        }
    }
}

fn main() -> i32 {
    test_continue();
    test_continue_tail_expr();
    test_break();
    test_break_tail_expr();
    test_break_value();
    test_while_continue();
    test_while_break();

    0
}
